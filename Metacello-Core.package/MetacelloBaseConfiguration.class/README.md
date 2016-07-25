You should be using ConfigurationOf instead of MetacelloBaseConfiguration. 

The class is being kept around because many extant Metacello configurations use MetacelloBaseConfiguration as a sentinel class to indicate whether or not Metacello is loaded..

Once the Metacello scripting API becomes prevalent (the api has it's own ensureMetacello logic) this class can be removed.