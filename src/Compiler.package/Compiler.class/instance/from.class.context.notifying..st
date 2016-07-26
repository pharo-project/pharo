private
from: textOrStream class: aClass context: aContext notifying: req
	sourceStream := textOrStream readStream.
	class := aClass.
	context := aContext.
	requestor := req