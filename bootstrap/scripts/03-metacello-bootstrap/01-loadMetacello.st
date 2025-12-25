| mcPackages |
mcPackages := #(
 'Metacello-Base'
 'Metacello-Core'
 'Metacello-CommandLine'
 ).

MonticelloBootstrap new loadPackagesNamed: mcPackages!