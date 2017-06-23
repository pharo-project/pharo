I am a helper to reset selections on changes tree browser.
My purpose is to allow weaks to work properly, since the browser needs to do something like this: 
	
	onChangeOfPort: #entity act: [ :presentation :diff | 
			diff ifNotNil: [ 
				diff announcer weak 
					subscribe: IceChangeSetChanged 
					do: [ presentation selection: nil ] ] ]

... but the problem is that blocks and weak announcements do not work well, because when declaring a block you take all the outer context with you, hence creating a de-facto strong reference . 
So we need a replacement, and I'm that replacement :)
Then, that code will look like this: 

	onChangeOfPort: #entity act: [ :presentation :diff | 
			diff ifNotNil: [ 
				diff announcer weak 
					subscribe: IceChangeSetChanged 
					send: #execute:
					to: (IceChangesTreeResetSelectionHelper for: presentation) ] ]

removing the outer context problems .