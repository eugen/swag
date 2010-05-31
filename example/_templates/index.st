<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en" >
  <head>
    <meta http-equiv="Content-Type" content="text/html;charset=utf-8" />
    <title>$title$ - Title</title>
    <link rel="stylesheet" type="text/css" href="static/style.css" />
    <link href="feed.xml" type="application/atom+xml" rel="alternate" title="ATOM Feed" />
  </head>
  <body>
    <div id="container">
      <div id="header">
        $header()$
        <div class="clear"></div>
      </div>
      <div id="content">
        <h2>$title$</h2>
        $content$
      </div>
      <div id="sidebar">
        $sidebar()$
      </div>
      <div class="clear"></div>
      <div id="footer">
        $footer()$
      </div>
    </div>
  </body>
</html>
