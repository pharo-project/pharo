If you want to register the same action for multiple events, simply create an AnnouncementSet using a comma: 

	Parent>>initialize 
	    super initialize. 
	    self session announcer on: AddChild, RemoveChild do: [:it | self changeChild: it child]
	
Motivation example: Often the UI is built after/independently from the model. You want to have the model raise fine-grained announcements to enable the layers on top, but sometimes it is easier in the UI to refresh everything whenever something happens.