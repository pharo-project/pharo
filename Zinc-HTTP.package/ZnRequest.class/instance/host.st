accessing
host
	| host scheme |
	host := self headers at: 'Host' ifAbsent: [ ^ nil ].
	scheme :=ZnCurrentServer value ifNil: [ #http ] ifNotNil: [ :server | server scheme ].
	^ ZnUrl fromString: host defaultScheme: scheme