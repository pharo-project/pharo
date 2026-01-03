| mcPackages |
mcPackages := #(
 'Metacello-Core'
 'Metacello-CommandLine'
 ).

MonticelloBootstrap new loadPackagesNamed: mcPackages!