<?xml version="1.0" encoding="utf-8"?>
 
<feed xmlns="http://www.w3.org/2005/Atom">
 
	<title>OMG TITLE</title>
	<subtitle>Subtitle.</subtitle>
	<link href="http://syntactic.org/feed.xml" rel="self" />
	<link href="http://example.org/" />
	<id>http://example.com/blog/index.html</id>
	<updated>$updatedAtom$</updated>
	<author>
		<name>Author Name</name>
		<email>johndoe@example.com</email>
	</author>
 
    $postTitles,postAddresses,postDatesAtom,postContents:{title,address,date,content | 
 	<entry>
		<title>$title$</title>
		<link rel="alternate" type="text/html" href="http://example.org/blog/$address$"/>
		<id>http://example.org/blog/$address$</id>
		<updated>$date$</updated>
		<content type="xhtml">
            <div xmlns="http://www.w3.org/1999/xhtml">
                $content$
            </div>
        </content>
	</entry>
    }$

</feed>
