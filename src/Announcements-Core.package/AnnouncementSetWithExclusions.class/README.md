I am an AnnouncementSet that explicitely does not handle a number of exclusion Announcements.

You could define me by #- message:

	announcer when: Announcement - ValueChanged do: [...].
	announcer when: Announcement - ValueChanged - ValueAdded do: [...].
	announcer when: ValueAdded, ValueRemoved - ValueAdded
 
Internal Representation and Key Implementation Points.

    Instance Variables
	exclusions:		<OrderedCollection>