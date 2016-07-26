lists behavior
selectedPluginClasses

	| associations list |
	list := self getPluginClassesList.
	pluginClassesSelected ifNil: [ ^ {} ].
	associations := pluginClassesSelected associations select: [:assoc | assoc value ].
	associations := associations collect: [:assoc | assoc key ].
	associations := associations sort: [:a : b | (list indexOf: a ifAbsent: [ 0 ]) <= (list indexOf: b ifAbsent: [ 0 ])].
	^ associations select: [:each | each notNil ]