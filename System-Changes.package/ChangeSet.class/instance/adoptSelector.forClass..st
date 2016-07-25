method changes
adoptSelector: aSelector forClass: aClass
	"Adopt the given selector/class combination as a change in the receiver"

	self noteNewMethod: (aClass compiledMethodAt: aSelector)
			forClass: aClass selector: aSelector priorMethod: nil