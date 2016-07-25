accessing
eventHandler
	"answer the receiver's eventHandler"
	^ extension ifNotNil: [extension eventHandler] 