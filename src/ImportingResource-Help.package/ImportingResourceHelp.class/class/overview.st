documentation
overview
	^ HelpTopic 
		title: 'Overview'
		contents: (self heading: 'Overview'),
'Non trivial applications often rely on resources that may be externally provided such as icons, pictures, sounds. Such resources are typically defined as external files. Naturally, you can make your application depends on external files, however, importing resource files within the Pharo image and make it integrally part of you code has a number of advantages:
- you can simply deploy your application by downloading it from smalltalkhub. 
- having less dependencies on external files is always good to avoid technical subtilities such as file path containing weird characters.
 '