event-handling
elementChanged
	self text doItReceiver: self selectedElement value.
	self text widget
		ifNotNil: [ :w | 
			w widget editingMode
				classOrMetaClass: self selectedElement value class ]