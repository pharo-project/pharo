accessing
url
	"It returns URL indicating where to send collected data."
	^ url ifNil: [ url := GTEventRecorderSettings url ]