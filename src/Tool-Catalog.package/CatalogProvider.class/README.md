I'm a provider for projects in the pharo catalog. Pharo projects are represented as CatalogProject instances. 

Usually  I fetch projects from http://catalog.pharo.org/catalog/json and create CatalogProject from such description. 

Use my default instance, I will cache the catalog data for 24 hours.

	CatalogProvider default projects.
