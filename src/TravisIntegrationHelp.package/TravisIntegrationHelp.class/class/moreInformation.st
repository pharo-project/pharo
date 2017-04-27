pages
moreInformation
	^ HelpTopic 
		title: 'More information'
		contents: '
Currently, there is no automatic way to start a build by simply committing to SmalltalkHub. The CronJob will run the test every 24 hours. You can manually run the build by either clicking on "start build" in Travis, or doing a commit in the git repository. 

Personally, I have created a short shell script, called update.sh, that simply does
	git commit --allow-empty -m "Trigger" && git push


This help is written by Alexandre Bergel (http://bergel.eu) with the help of Juraj Kubelka. 

Big thanks to the HPI team that has produced the Pharo support. More information about the Pharo support on Travis may be found on:
https://github.com/hpi-swa/smalltalkCI

'