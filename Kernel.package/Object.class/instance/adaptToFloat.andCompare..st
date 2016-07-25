converting
adaptToFloat: rcvr andCompare: selector 
	"If I am involved in comparison with a Float.
	Default behaviour is to process comparison as any other selectors."
	^ self adaptToFloat: rcvr andSend: selector