I'm a monticello tool composed of three panes to browse repositories and the packages they contain. You get an instance of me when you click on a repository in Monticello browser and press open. 

My left pane presents the packages, my right one their versions and the bottom one the commit log of the selected package versions.

I underline the packages you already loaded, and highlight the ones you don't have updated to the last version. I also highlight the versions you did not load yet.

Example: I can browse packages of PharoInbox with:
(MCFileRepositoryInspector 
	repository: (MCHttpRepository
					location: 'http://www.squeaksource.com/PharoInbox'
					user: ''
					password: '')
	workingCopy: nil) show.
	
COTDC - S.Ducasse, G.Polito, L.Laffont