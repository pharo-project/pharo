desktop
desktopBackgroundChanged
	| fill |
	
	self desktopImageFileName
		ifEmpty: [UIManager default currentWorld world backgroundMorph: nil]
		ifNotEmpty: [ [
				World backgroundImage: (Form fromFileNamed: self desktopImageFileName) layout: self desktopImageLayout 
			] on: Error	do: []
		].
	
	self useDesktopGradientFill
		ifTrue: [World fillStyle isGradientFill
				ifTrue: [fill := World fillStyle]
				ifFalse: [fill := GradientFillStyle new].
			fill colorRamp: {0.0 -> self desktopColor darker. 1.0 -> self desktopGradientFillColor lighter}.
			fill origin: (World perform: self desktopGradientOrigin).
			self desktopGradientDirection = #Vertical
				ifTrue: [fill radial: false.
					fill normal: World width @ 0.
					fill direction: 0 @ World height].
			self desktopGradientDirection = #Horizontal
				ifTrue: [fill radial: false.
					fill normal: 0 @ World height.
					fill direction: World width @ 0]. 
			self desktopGradientDirection = #Radial
				ifTrue: [
					fill radial: true.
					self desktopGradientOrigin = #center
						ifTrue: [fill direction: World width // 2 @ 0.
							fill normal: 0 @ (World height // 2)]
						ifFalse: [fill direction: World width @ 0.
							fill normal: 0 @ World height]].
			World fillStyle: fill]
		ifFalse: [World color: self desktopColor].
		
		World changed