initialize-release
initialize 
	| ref loadFileFailBlock|
	super initialize.
	self extent: self defaultExtent.
	self color: self backColor.
	surface := AthensCairoSurface extent: self extent.
	current := 1.
	frame := 0.
      loadFileFailBlock:= AthensCairoSurface fromForm:(Form fromDisplay: (0@0 extent:(300@150))).
	ref := 	'Pharo.png' asFileReference.
	pharoLogo := AthensCairoSurface createFromFile:  ref fullName ifFailed:  [ loadFileFailBlock ].

	ref := 'esug-balloon.png' asFileReference.
	esugBallon := AthensCairoSurface createFromFile:  ref fullName ifFailed:  [ loadFileFailBlock ].

	
	self startStepping.
	