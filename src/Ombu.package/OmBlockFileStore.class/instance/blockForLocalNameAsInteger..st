private
blockForLocalNameAsInteger: anIndex
	
	blocks reverseDo: [:each | each firstLocalNameAsInteger <= anIndex ifTrue: [ ^each ] ].
	^ self error: 'block not found'