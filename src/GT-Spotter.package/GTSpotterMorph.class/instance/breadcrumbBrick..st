accessing
breadcrumbBrick: aBrick

	breadcrumbBrick ifNotNil: [self removeBrick: breadcrumbBrick].
	breadcrumbBrick := aBrick.
	
	breadcrumbBrick announcer when: GTSpotterBreadcrumbVisibilityChanged send: #onBreadcrumbVisibilityChanged to: self.
	self themer spotterThemer breadcrumbStyleFor: breadcrumbBrick.
	breadcrumbBrick width: [ self paneBrick contentsBrick width ].
	breadcrumbBrick layout widthDependency: [ self paneBrick contentsBrick ].
	self addBrickBack: breadcrumbBrick