<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en" >
  <head>
    <meta http-equiv="Content-Type" content="text/html;charset=utf-8" />
    <title>$title$ - Title</title>
    <link rel="stylesheet" type="text/css" href="static/style.css" />
    <link rel="stylesheet" type="text/css" href="../static/style.css" />
    <link href="feed.xml" type="application/atom+xml" rel="alternate" title="ATOM Feed" />
  </head>
  <body>
    <div id="header">
      $header()$
    </div>
    <div id="content">
      <h2>Articles / Posts</h2>
      <ul class="articles">
      $postTitles,postAddresses,postDates:{title,address,date | 
        <li>
          <h3>
            <a href="$address$">$title$</a>
          </h3>
          <sup>$date$</sup>
        </li>
      }$
      </ul>
      $content$
    </div>
    <div id="sidebar">
      $sidebar()$
    </div>
    <div id="footer">
      $footer()$
    </div>
  </body>
</html>
