instance creation
for: anAnnouncementClass where: aBlock

	^self new 
		announcementClass: anAnnouncementClass;
		conditionBlock: aBlock