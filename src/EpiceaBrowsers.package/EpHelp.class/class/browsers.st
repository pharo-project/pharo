documentation
browsers
	^ HelpTopic
		title: 'Browsers'
		contents: '
BROWSERS

Epicea provides browsers to visualize and manipulate the code changes and IDE events that the Monitor logs.


SESSION CHANGES

It shows the code changes since Epicea started to log. This means it shows changes placed in potentially several ombu files.


ALL CHANGES

The windows shows:
- all the logs in the local directory on the left panel, and 
- the content of the selected log in the right panel.

This window visually connects the logs to help user to understand how the ombu files are related. Note that the code changes displayed on the right panel belong to only one log (different than in the Monitor UI, that might show changes in several logs).
'