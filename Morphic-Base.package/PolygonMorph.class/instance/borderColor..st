accessing
borderColor: aColor 

	super borderColor: aColor.
	(borderColor isColor and: [borderColor isTranslucentButNotTransparent]) 
		== (aColor isColor and: [aColor isTranslucentButNotTransparent]) 
			ifFalse: 
				["Need to recompute fillForm and borderForm
					if translucency of border changes."

				self releaseCachedState ]