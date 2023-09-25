
Imports System
Imports System.Data.DataSet
Imports System.IO
Imports System.Web
Imports System.Xml
Imports System.Net
Imports System.Xml.XPath
Imports System.Text

Public Class LCDSmartie
    'http://geofon.gfz-potsdam.de/eqinfo/list.php?latmin=&latmax=&lonmin=&lonmax=&magmin=&fmt=rss

    Dim rssUrl As String = "http://geofon.gfz-potsdam.de/eqinfo/list.php?latmin=&latmax=&lonmin=&lonmax=&magmin=&fmt=rss"

    Dim completetitle As String
    Dim completedescription As String
    Dim title(20)
    Dim description(20)
    Dim lasttick = 0
    Dim retrieving As Boolean = False
    Dim checktime = 0
    Dim timechanged = False


    Private Sub RssGoTitle()
        completetitle = "#"
        Try
            ' set the file path member var
            ' create a new xml doc
            Dim doc As New XmlDocument()
            Try
                ' load the xml doc
                doc.Load(rssUrl)
               
            Catch ex1 As Exception
        
                Return
            End Try
            ' get an xpath navigator   
            Dim navigator As XPathNavigator = doc.CreateNavigator()
            Try
                ' look for the path to the rss item titles navigate
                ' through the nodes to get all titles
                Dim nodes As XPathNodeIterator = navigator.Select("/rss/channel/item/title")
                While nodes.MoveNext
                    ' clean up the text for display
                    Dim node As XPathNavigator = nodes.Current
                    Dim tmp As String = node.Value.Trim()
                    tmp = tmp.Replace(ControlChars.CrLf, "")
                    tmp = tmp.Replace(ControlChars.Lf, "")
                    tmp = tmp.Replace(ControlChars.Cr, "")
                    tmp = tmp.Replace(ControlChars.FormFeed, "")
                    tmp = tmp.Replace(ControlChars.NewLine, "")
                    'Label2.Text = ""
                    '  Label2.Text += " * " & tmp
                    completetitle += " * " & tmp
                End While
                ' set a position counter
                Dim position As Integer = 0
            Catch ex As Exception
      
            End Try
   
        Catch ex2 As Exception
          
        End Try

    End Sub

    Private Sub RssGoDescription()
        completedescription = "#"
        Try
            ' set the file path member var
            ' create a new xml doc
            Dim doc As New XmlDocument()
            Try
                ' load the xml doc
                doc.Load(rssUrl)
                ' return the cursor
                '  Me.Cursor = Cursors.Default
            Catch ex1 As Exception
              
                Return
            End Try
            ' get an xpath navigator   
            Dim navigator As XPathNavigator = doc.CreateNavigator()
            Try
                ' look for the path to the rss item titles navigate
                ' through the nodes to get all titles
                Dim nodes As XPathNodeIterator = navigator.Select("/rss/channel/item/description")
                While nodes.MoveNext
                    ' clean up the text for display
                    Dim node As XPathNavigator = nodes.Current
                    Dim tmp As String = node.Value.Trim()
                    tmp = tmp.Replace(ControlChars.CrLf, "")
                    tmp = tmp.Replace(ControlChars.Lf, "")
                    tmp = tmp.Replace(ControlChars.Cr, "")
                    tmp = tmp.Replace(ControlChars.FormFeed, "")
                    tmp = tmp.Replace(ControlChars.NewLine, "")
                    'Label2.Text = ""
                    '    Label2.Text += " * " & tmp
                    completedescription += " * " & tmp
                End While
                ' set a position counter
                Dim position As Integer = 0
            Catch ex As Exception
            
            End Try
     
        Catch ex2 As Exception
          
        End Try

    End Sub


    Public Declare Function GetTickCount Lib "kernel32" () As Long


    Public Function function1(ByVal param1 As String, ByVal param2 As String) As String
        On Error GoTo ErrH

        If param1 = "about" Or param2 = "about" Then
            Return " function 1 returns earthquake info as provided by GEOFON site type 'help' for more info"

        ElseIf param1 = "help" Or param2 = "help" Then
            Return " check info text"

        Else
            ' If lasttick < GetTickCount + 10000 Then
            ' retrieving = True
            ' RssGoTitle()
            ' RssGoDescription()
            ' lasttick = GetTickCount
            ' Else
            ' retrieving = False
            ' Return "refresh"
            ' End If

            Dim time As DateTime = DateTime.Now
            Dim format As String = "ss"
            If timechanged = False Then
                timechanged = True
                SaveSetting("earthq2", "settings", "timetocheck", time.ToString(format))
                'completetitle = "-*-"
                'completedescription = "-*-"
                RssGoTitle()
                RssGoDescription()
            End If

            If time.ToString(format) = GetSetting("earthq2", "settings", "timetocheck", 0) Then
                ' completetitle = "-*-"
                '  completedescription = "-*-"
                RssGoTitle()
                RssGoDescription()
                'Return "-"
            End If



            title = Split(completetitle, "*")
            '  Label3.Text = 
            description = Split(completedescription, "*")

            If LCase(param2) = "magnitude" Or LCase(param2) = "mag" Or LCase(param2) = "mg" Or LCase(param2) = "magni" Then
                Dim out = Split(title(param1), ",")
                out(0) = Trim(Replace(out(0), "M", ""))

                Return out(0)
            ElseIf LCase(param2) = "location" Or LCase(param2) = "loc" Or LCase(param2) = "lc" Or LCase(param2) = "locate" Then

                Dim out = Split(title(param1), ",")
                out(1) = Trim(out(1))

                Return out(1)


            ElseIf LCase(param2) = "depth" Or LCase(param2) = "dep" Or LCase(param2) = "dp" Then
                Dim clearout = Replace(description(param1), "  ", " ")
                clearout = Replace(clearout, "  ", " ")
                Dim out2 = Split(clearout, " ")
                out2(5) = Trim(out2(5))

                Return out2(5)


            ElseIf LCase(param2) = "origintime" Or LCase(param2) = "datetime" Or LCase(param2) = "time" Or LCase(param2) = "tm" Then
                Dim clearout = Replace(description(param1), "  ", " ")
                clearout = Replace(clearout, "  ", " ")
                Dim out3 = Split(clearout, " ")
                out3(1) = Trim(out3(1))

                Return out3(1) & " " & out3(2)
            Else

                Return title(param1) & description(param1)


            End If
        End If



        Exit Function
ErrH:

        Return ""
    End Function


    

    Public Function function19(ByVal param1 As String, ByVal param2 As String) As String
        On Error GoTo errh

        Dim time As DateTime = DateTime.Now
        Dim format As String = "ss"
        If timechanged = False Then
            timechanged = True
            SaveSetting("earthq2", "settings", "timetocheck", time.ToString(format))

        End If

        Return "Diagnostics: " & time.ToString(format) & " " & GetSetting("earthq2", "settings", "timetocheck", 0)

        Exit Function
errh:
        Return ""


    End Function



    Public Function function20(ByVal param1 As String, ByVal param2 As String) As String

        If param1 = "about" Or param2 = "about" Then
            Return "  Author Nikos Georgousis " 'credits 
        ElseIf param1 = "plugin" Or param2 = "plugin" Then
            Return " earthquake 2"
        Else
            Return " earthq2.dll version 1.1 by Limbo. A plugin for LCD Smartie" ' about

        End If

    End Function


    Public Function SmartieInfo()

        Return "Developer:Nikos Georgousis (Limbo)" & vbNewLine & "Version:1.1"

    End Function


    Public Function SmartieDemo()
        Dim demolist As New StringBuilder()
        demolist.AppendLine("----function 1----")
        demolist.AppendLine("> Returns earthquake info from GEOFON <")
        demolist.AppendLine("latest event $dll(earthq2,1,1,)")
        demolist.AppendLine(" ")
        demolist.AppendLine("Filters")
        demolist.AppendLine("Event date/time $dll(earthq2, 1, 1, origintime)")
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
