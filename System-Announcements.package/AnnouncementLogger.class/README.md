Use me to debug and log to the transcript annoncements.


AnnouncementLogger new subscribeTo: SystemAnnouncer announcer

To unsubscribe

AnnouncementLogger  allInstancesDo: [:each | SystemAnnouncer announcer unsubscribe: each ]