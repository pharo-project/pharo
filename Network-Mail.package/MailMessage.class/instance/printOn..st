printing
printOn: aStream 
	"For text parts with no filename show: 'text/plain: first line of text...'    
	for attachments/filenamed parts show: 'attachment: filename.ext'"

	| name |

	aStream nextPutAll: ((name := self name) ifNil: ['Text: ' , self excerpt]
			ifNotNil: ['File: ' , name])