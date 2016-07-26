lessons
reflectionContinued
	^ Lesson
title: 'Reflection continued' 
lesson: 
'"So cool, isn''t it ?  Before going further, let''s remove this method:"

ProfStef respondsTo: #goToNextLesson.

ProfStef class removeSelector: #goToNextLesson.

ProfStef respondsTo: #goToNextLesson.


"Then move forward:"

ProfStef default executeMethod: (ProfStef lookupSelector:#next).'