registering fills
registerFormFill: form origin: orig direction: dir normal: norm tileFlag: tileFlag

	forms := forms copyWith: form.
	form unhibernate.
	currentFill := self primAddBitmapFill: form
			colormap: (form colormapIfNeededForDepth: 32)
			tile: tileFlag
			from: orig
			along: dir
			normal: norm
			xIndex: forms size.
			
