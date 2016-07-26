subscription
subscribe: anAnnouncementClass do: aValuable 
	"Declare that when anAnnouncementClass is raised, aValuable is executed."
	^ registry add: (
		AnnouncementSubscription new 
			announcer: self;
			announcementClass: anAnnouncementClass;
			valuable: aValuable)