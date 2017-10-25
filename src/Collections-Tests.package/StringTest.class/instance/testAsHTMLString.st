tests
testAsHTMLString

self assert: ('<a>' asHTMLString) = '&lt;a&gt;'.
self assert: ('<!DOCTYPE html>' asHTMLString) = '&lt;!DOCTYPE html&gt;'. 
self assert: ('é è à ô ü ö ß ä ó ñ í á' asHTMLString) = 'é è à ô ü ö ß ä ó ñ í á'.
self assert: ('' asHTMLString) = ''.