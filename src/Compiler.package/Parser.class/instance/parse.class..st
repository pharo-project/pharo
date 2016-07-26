public access
parse: sourceStreamOrString class: behavior

	^ self parse: sourceStreamOrString readStream class: behavior
		noPattern: false context: nil notifying: nil ifFail: [ self fail ]