fileHandling
selectTextConverterForCode
	self flag: #fix.  "This should not be here probably."
	
	"We need to see the first three bytes in order to see the origin of the file"
	readStream binary.
	((readStream next: 3) = #[ 16rEF 16rBB 16rBF ]) ifTrue: [
		readStream converter: UTF8TextConverter new
	] ifFalse: [
		readStream converter: MacRomanTextConverter new.
	].

	"we restore the position to the start of the file again"
	readStream position: 0.
	
	"We put the file in text mode for the file in"
	readStream text.