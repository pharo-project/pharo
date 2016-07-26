private
accept: text notifying: notifyer

	^listModel selectedItem
		ifNotNil: [:message | 
			message methodClass 
				compile: text 
				classified: message protocol
				notifying: notifyer ]