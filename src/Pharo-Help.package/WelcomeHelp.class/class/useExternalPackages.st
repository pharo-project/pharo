pages
useExternalPackages
	^ HelpTopic 
		title: 'Using Eternal Packages'
		contents: (self heading: 'Using External Packages'), 'Pharo already comes pre-loaded with a rich set of packages that you can use to explore the system and develop your own applications. However there is also a huge library of user contributed projects that you can also load using the "Catalog Browser". To open it click on the following expression:
		
', (self doItFrom: 'CatalogBrowser open.'), '

This browser is also accessible from the World>Tools>Catalog Browser menu (just click the desktop, select Tools, and then Catalog Browser).

Catalog projects can be browsed online: 

', (self url: 'http://catalog.pharo.org'), '

You can also a rough list (not very friendly) of many packages available stored in smalltalkhub repository:

', (self url: 'http://smalltalkhub.com/list'), '

(this list is not a compendium of packages for Pharo but is a good place to start looking for something in particular and which happens to not be present in the catalog).

The catalog can be enabled in Spotter by enabling the "Display catalog projects in Spotter" system preference.
'