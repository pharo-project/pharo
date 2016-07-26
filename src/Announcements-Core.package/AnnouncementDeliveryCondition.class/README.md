I represent condition which should be satisfied on given announcement to be delivered to subscription.
I am used to implement instance specific subscription on announcements. 

To create me send message #where to announcement class with condition block:

	Announcement where: [ :ann | ann param = #expected ]

I can be used directly in announcer subscription API in place of announcement class:

	announcer when:  (ValueChanged where: [:change | change oldValue = 100])

Be carefull with me because blocks in subscriptions produce references to outer contexts (recever and all senders)
 
Internal Representation and Key Implementation Points.

    Instance Variables
	announcementClass:		<Announcement class>
	conditionBlock:		<BlockClosure>