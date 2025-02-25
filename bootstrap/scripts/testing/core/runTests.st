ClassTestCase removeSelector: #testClassComment.
HashTesterTest removeSelector: #testBasicBehaviour.


(Package organizer packages collect: #name) logCr.

HDTestReport runPackages: (Package organizer packages collect: #name).

"HDTestReport runClasses: Smalltalk allClasses named: 'Tests'."

Smalltalk exitSuccess. 