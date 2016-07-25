system announcements
classCommented: anAnnouncement
    " announcement handled when a class has been commented " 
    | aClass |

    window ifNil: [ ^ self ].
    window isDisplayed ifFalse: [ ^ self ].

    aClass := anAnnouncement classCommented.    
    (self classWidget getClassesList includes: aClass)
        ifTrue: [ self updateClassView ]