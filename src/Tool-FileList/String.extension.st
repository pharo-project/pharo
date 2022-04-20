Extension { #name : #String }

{ #category : #'*Tool-FileList' }
String >> asFileName [
	"Answer a String made up from the receiver that is an acceptable file name."

	^ FileSystem disk checkName: self fixErrors: true
	 
]
