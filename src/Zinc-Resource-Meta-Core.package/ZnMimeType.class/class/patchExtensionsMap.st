private
patchExtensionsMap
	"Patch the extension map that we read from the spec"
	
	#('~' '%') do: [ :each | ExtensionsMap removeKey: each ].
	(ExtensionsMap at: 'text') setCharSetUTF8.
	(ExtensionsMap at: 'html') setCharSetUTF8.
	ExtensionsMap at: 'asc' put: 'text/plain' asZnMimeType