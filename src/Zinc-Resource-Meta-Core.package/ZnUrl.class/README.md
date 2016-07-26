I am ZnUrl, an implementation of an interpreted URL/URI.
URLs are an element used in describing resources, more specifically to identify them.

I consist of the following parts:
  - scheme - like #http, #https, #ws, #wws, #file or nil
  - host - hostname string or nil
  - port - port integer or nil
  - segments - collection of path segments, ends with #/ for directories
  - query - query dictionary or nil
  - fragment - fragment string or nil
  - username - username string or nil
  - password - password string or nil

The syntax of my external representation informally looks like this

  scheme://username:password@host:port/segments?query#fragment

I am most often created by parsing my external representation using either my #fromString: class method or by sending the #asZnUrl convenience method to a String. Using #asZnUrl helps in accepting both Strings and ZnUrls arguments.

  ZnUrl fromString: 'http://www.google.com/search?q=Smalltalk'.

I can also be constucted programmatically.

  ZnUrl new 
    scheme: #https; 
    host: 'encrypted.google.com'; 
    addPathSegment: 'search'; 
    queryAt: 'q' put: 'Smalltalk'; 
    yourself.
  
My components can be manipulated destructively. Here is an example:

  'http://www.google.com/?one=1&two=2' asZnUrl
    queryAt: 'three' put: '3';
    queryRemoveKey: 'one';
    yourself.

Some characters of parts of a URL are illegal because they would interfere with the syntax and further processing and thus have to be encoded. The methods in accessing protocols do not do any encoding, those in parsing and printing do. Here is an example:

  'http://www.google.com' asZnUrl
    addPathSegment: 'some encoding here';
    queryAt: 'and some encoding' put: 'here, too';
    yourself

My parser is somewhat forgiving and accepts some unencoded URLs as well, like most browsers would.

  'http://www.example.com:8888/a path?q=a, b, c' asZnUrl.

I can parse in the context of a default scheme, like a browser would do.

  ZnUrl fromString: 'www.example.com' defaultScheme: #http

Given a scheme, I know its default port, try #portOrDefault.

A path defaults to what is commonly referred to as slash, test with #isSlash. Paths are most often (but don't have to be) interpreted as filesystem paths. To support this, I have #isFilePath and #isDirectoryPath tests and #file and #directory accessors.

I have some support to handle one URL in the context of another one, this is also known as a relative URL in the context of an absolute URL. Refer to #isAbsolute, #isRelative and #inContextOf:

  '/folder/file.txt' asZnUrl inContextOf: 'http://fileserver.example.net:4400' asZnUrl.

Incomplete relative references can be parsed and resolved in the context of a base URL using #withRelativeReference:

  'http://www.site.com/static/html/home.html' asZnUrl withRelativeReference: '../js/menu.js'.

Sometimes, the combination of my host and port are referred to as authority, see #authority.

URL/URI/URN (Uniform/Universal Resource Locator/Identifier/Name) are closely related and can be and are used as synonyms is many contexts. Refer to http://en.wikipedia.org/wiki/Url for more information.

There is a convenience method #retrieveContents to download the resource a ZnUrl points to,

  'http://zn.stfx.eu/zn/numbers.txt' asZnUrl retrieveContents.

This is implemented using a ZnUrlOperation.
 
Part of Zinc HTTP Components.