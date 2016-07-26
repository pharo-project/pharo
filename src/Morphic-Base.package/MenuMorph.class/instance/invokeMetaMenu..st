private
invokeMetaMenu: evt
	stayUp ifFalse:[^self]. "Don't allow this"
	^super invokeMetaMenu: evt