lessons
createLessonFactoryMethods
	^ Lesson 
title: 'Lesson factory methods'
lesson: 
'"Next, create category ''lessons'' and add a method per lesson.

Each method must return a Lesson object.

Your can use Lesson class>>title:lesson: to create Lesson object."

HowToDebug compile:
''useSelfHalt
	^ Lesson
title: ''''self halt''''
lesson: 
''''"Put self halt. in the method you want to debug."

ProfStef next.''''''
classified: ''lessons''.

HowToDebug compile:
''examineStackTrace
	^ Lesson
title: ''''self halt''''
lesson: 
''''"Look at PharoDebug.log."

ProfStef next.''''''
classified: ''lessons''.

HowToDebug compile:
''changeReturnValue
	^ Lesson
title: ''''Change return value''''
lesson: 
''''"Easy in the debugger !"''''''
classified: ''lessons''.

ProfStef next.'.