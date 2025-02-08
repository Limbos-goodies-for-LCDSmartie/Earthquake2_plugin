

Imports System
Imports System.IO
Imports System.Net
Imports System.Xml
Imports System.Text
Imports System.Text.RegularExpressions
Imports System.Runtime.InteropServices.WindowsRuntime
Imports System.Diagnostics.Eventing.Reader

Public Class LCDSmartie
    Private rssUrl As String = "https://geofon.gfz.de/eqinfo/list.php?&fmt=rss"
    Private lastUpdateTime As Long = 0
    Private title As New List(Of String)()
    Private description As New List(Of String)()
    Private preVDescriptionDetails As String = ""

    Private Function ShouldUpdate() As Boolean
        Dim currentTime As Long = Environment.TickCount
        If currentTime - lastUpdateTime >= 60000 OrElse lastUpdateTime = 0 Then
            lastUpdateTime = currentTime
            SaveSetting("earthq2", "Settings", "lastUpdateTime", lastUpdateTime)
            Return True
        End If
        Return False
    End Function

    Private Sub UpdateRSS()

        ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12

        Try
            ' Force modern TLS support (important for HTTPS requests)
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12

            Dim request As WebRequest = WebRequest.Create(rssUrl)
            Using response As WebResponse = request.GetResponse()
                Using reader As New StreamReader(response.GetResponseStream())
                    Dim xmlContent As String = reader.ReadToEnd()

                    ' Save XML content to a file for debugging
                    '     Dim debugFilePath As String = "C:\RSS_Debug.xml"
                    '   File.WriteAllText(debugFilePath, xmlContent)

                    ' Load into XML document
                    Dim doc As New XmlDocument()
                    doc.LoadXml(xmlContent)

                    Dim titleNodes = doc.SelectNodes("/rss/channel/item/title")
                    Dim descNodes = doc.SelectNodes("/rss/channel/item/description")

                    title.Clear()
                    description.Clear()

                    For i As Integer = 0 To Math.Min(titleNodes.Count - 1, 20)
                        title.Add(titleNodes(i).InnerText)
                        description.Add(descNodes(i).InnerText)
                    Next

                End Using
            End Using
        Catch ex As Exception
            ' Save error details to a file
            Dim errorLogPath As String = "C:\RSS_Debug_Error.txt"
            File.WriteAllText(errorLogPath, "Error: " & ex.Message & vbNewLine & ex.StackTrace)
            '  Return "Error occurred!"
        End Try


    End Sub



    Public Function ExtractDepth(desc As String) As String
        Dim match As Match = Regex.Match(desc, "\d+\s+km")
        If match.Success Then
            Return match.Value
        Else
            Return "N/A"
        End If
    End Function


    Public Function function1(ByVal param1 As String, ByVal param2 As String) As String

        If param1 = "about" Or param2 = "about" Then
            Return "returns 1 on new event"
        End If

        Try
            If ShouldUpdate() Then
                UpdateRSS()
                '   Dim debugTestPath As String = "C:\RSS_Debug_Test.txt"
                '     File.WriteAllText(debugTestPath, "Function1 was called!")
            End If

            'If param1 = "about" Then
            '    Return "Earthquake RSS Data Retriever"
            'End If

            Dim index As Integer = Integer.Parse(param1) - 1
            If index < 0 OrElse index >= title.Count Then Return "Error: Index out of range"

            Select Case LCase(param2)
                Case "magnitude", "mag", "mg", "magni"
                    Dim out = title(index).Split(","c)
                    Return Trim(Replace(out(0), "M", ""))
                Case "location", "loc", "lc"
                    Dim out = title(index).Split(","c)
                    Return Trim(out(1))
                Case "origintime", "time", "datetime", "tm"
                    Dim descDetails = description(index).Split(" "c)
                    Return descDetails(0) & " " & descDetails(1)

                Case "depth", "dep", "dp"
                    Dim depth As String = ExtractDepth(description(index))
                    Return depth

                Case Else
                    Return "Error: Invalid parameter"
            End Select
        Catch ex As Exception
            Return "Error: " & ex.Message
        End Try
    End Function



    Public Function function2(ByVal param1 As String, ByVal param2 As String) As String

        If param1 = "about" Or param2 = "about" Then
            Return "accepted parameters are 'mg', 'lc', 'tm', 'dp' "
        End If

        Try
            If ShouldUpdate() Then
                UpdateRSS()
                '   Dim debugTestPath As String = "C:\RSS_Debug_Test.txt"
                '     File.WriteAllText(debugTestPath, "Function1 was called!")
            End If

            'If param1 = "about" Then
            '    Return "Earthquake RSS Data Retriever"
            'End If


            Dim descDetails = description(0).Split(" "c)
            If descDetails(0) & " " & descDetails(1) <> preVDescriptionDetails Then
                preVDescriptionDetails = descDetails(0) & " " & descDetails(1)
                Return 1
            Else
                Return 0

            End If

        Catch ex As Exception
            Return "Error: " & ex.Message
        End Try
    End Function


    Public Function function19(ByVal param1 As String, ByVal param2 As String) As String
        If param1 = "about" Or param2 = "about" Then
            Return "accepted parameters are 'r' or nothing "
        End If


        If param1 = "r" Or param1 = "rem" Then
            Dim currentTime As Long = Environment.TickCount
            Return "Remaining time: " & 60000 - (currentTime - lastUpdateTime)
        Else
            Return "Last update: " & lastUpdateTime

        End If



    End Function

    Public Function function20(ByVal param1 As String, ByVal param2 As String) As String

        If param1 = "about" Or param2 = "about" Then
            Return "  Author Nikos Georgousis " 'credits 
        ElseIf param1 = "plugin" Or param2 = "plugin" Then
            Return " earthquake 2"
        Else
            Return " earthq2.dll version 2.2 by Limbo. A plugin for LCD Smartie" ' about

        End If

    End Function



    Public Function SmartieInfo()

        Return "Developer:Nikos Georgousis (Limbo)" & vbNewLine & "Version:2.2"

    End Function


    Public Function SmartieDemo()
        Dim demolist As New StringBuilder()
        demolist.AppendLine("----function 1----")
        demolist.AppendLine("> Returns earthquake info from GEOFON <")
        demolist.AppendLine("latest event $dll(earthq2,1,1,)")
        demolist.AppendLine(" ")
        demolist.AppendLine("Filters")
        demolist.AppendLine("Event date/time $dll(earthq2,1,1,origintime)")
        demolist.AppendLine("Location $dll(earthq2,1,1,location)")
        demolist.AppendLine("Magnitude $dll(earthq2,1,1,magnitude)")
        demolist.AppendLine("Depth $dll(earthq2,1,1,depth)")
        '   demolist.AppendLine(" ")
        ' demolist.AppendLine("_____________")
        demolist.AppendLine("Shorter calls")
        demolist.AppendLine("location = loc = locate = lc")
        demolist.AppendLine("magnitude = magni = mag = mg ")
        demolist.AppendLine("origintime = time = datetime = tm")
        demolist.AppendLine("depth = dep = dp")
        demolist.AppendLine(" ")
        demolist.AppendLine("Up to 10 events are supported")
        demolist.AppendLine("Third event $dll(earthq2,1,3,loc)")

        'demolist.AppendLine(" ")
        'demolist.AppendLine("----function 2----")
        'demolist.AppendLine("> Returns station name <")
        'demolist.AppendLine("station (clear) $dll(xtremp3,2,clear,)")
        'demolist.AppendLine("station $dll(xtremp3,2,,)")
        'demolist.AppendLine(" ")
        'demolist.AppendLine("----function 3----")
        'demolist.AppendLine("> Reserved <")
        'demolist.AppendLine("nothing $dll(xtremp3,3,,)")
        'demolist.AppendLine(" ")
        'demolist.AppendLine("----function 19----")
        'demolist.AppendLine("> Returns status <")
        'demolist.AppendLine("$dll(xtremp3,19,,)")
        'demolist.AppendLine("text output $dll(xtremp3,19,text,)")
        demolist.AppendLine(" ")
        demolist.AppendLine(" ")
        demolist.AppendLine("Updated on Feb 2025 during Santorini earthquakes")

        'demolist.AppendLine("_____________")
        demolist.AppendLine("----function 20----")
        demolist.AppendLine("> Credits <")
        demolist.AppendLine("About $dll(earthq2,20,,)")

        Dim result As String = demolist.ToString()
        Return result

    End Function




    Public Function GetMinRefreshInterval() As Integer

        Return 1000 ' refresh interval

    End Function

End Class
