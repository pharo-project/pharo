I represent an event recorder, an object that collects data and regularly sends them to a server. The data could represent user activity, tool usage, or any other statistic usefule for further analysis. 

I cover data collectors, packing, and delivery objects the way they cooperate together and the collected data are regularly send to server(s). It is not mandatory, but it is supposed that I represent a Singleton pattern.

I collaborate with GTEventCollector objects, that are responsible for collecting data. I keep any number of those objects and whenever GTEventDelivery asks me for collected data, GTEventPacking object packs data from each GTEventCollector and hand them as collection of GTEventBundle objects to the GTEventDelivery. GTEventDelivery sends the bundles to server(s).

Public API and Key Messages

- addCollector: it adds GTEventCollector object and activates delivery process if inactive   
- removeCollector: it removes  GTEventCollector object and deactivates delivery process if there are no other collectors.
- activateDelivery: activates delivery process
- deactivateDelivery: deactivates delivery process

Internal Representation and Key Implementation Points.

    Instance Variables
	announcer:		<Annoucner>
	collectors:		<GTEventCollectors>
	delivery:		<GTEventDelivery>
	packing:		<GTEventPacking>
	privacy:		<true|false|ClosureBlock>
