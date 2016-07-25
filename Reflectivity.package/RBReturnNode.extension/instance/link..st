*Reflectivity
link: aMetaLink
	(#(instead after) includes: aMetaLink control) ifTrue: [ self error: '#intead on return not supported' ].
	super link: aMetaLink 