accessing
title

	^ self manager selectedTab
		ifNil: [ 'Tabs Example' ]
		ifNotNil: [:tab | tab label ]