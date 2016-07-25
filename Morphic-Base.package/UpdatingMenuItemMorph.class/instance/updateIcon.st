private
updateIcon

	target ifNotNil: [ self icon: (target perform: iconSelector) ]