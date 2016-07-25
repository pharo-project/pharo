private
setOptionsPanel: aBoolean
	"This is a private method. The implementation is based on the fact the options are set by default"
	
	aBoolean ifFalse: [ self removeOptionsPanel ]