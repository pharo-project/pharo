accessing
port: aSymbol
	"returns the port with the given name"
	^ ports detect: [ :each | each name = aSymbol ] ifNone: [
		ports addLast: (
			(self class portClass new) 
						pane: self; 
						name: aSymbol; 
						yourself) ]