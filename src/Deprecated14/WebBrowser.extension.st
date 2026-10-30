Extension { #name : 'WebBrowser' }

{ #category : '*Deprecated14' }
WebBrowser class >> initialize [
	"Renamed in Pharo 14"

	self deprecatedAliases: { #WBMacWebBrowser. #WBUnixWebBrowser. #WBWindowsWebBrowser }
]
