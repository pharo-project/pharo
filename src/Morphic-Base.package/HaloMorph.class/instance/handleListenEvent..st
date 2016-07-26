events-processing
handleListenEvent: anEvent
	"We listen for possible drop events here to add back those handles after a dup/grab operation"

	(anEvent isMouse and:[anEvent isMove not]) ifFalse:[^ self]. "not interested"
	anEvent hand removeMouseListener: self. "done listening"
	(self world ifNil: [target world]) ifNil: [^ self].
	self addHandles  "and get those handles back"