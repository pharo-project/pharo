instance creation
createCollapsableArray: anInteger

	^ (1 to: anInteger) collect: [ :index | self createCollapsable hSpaceFill  ]