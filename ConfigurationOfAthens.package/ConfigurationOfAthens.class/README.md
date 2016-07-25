"Load latest stable version "
self loadStable.

"Load latest development version"

self loadDevelopment 


Extras:

Tutorial
self project lastVersion load: 'Tutorial'

"SVG importer"
(note before doing this you need to load XMLParser,
which you can find in Configurations browser)

self project lastVersion load: 'SVG'

