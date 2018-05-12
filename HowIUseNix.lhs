% Cartography in Haskell
% Dominic Steinitz
% 7th May 2018

Introduction
============

Suppose you want to analyze your local election results and visualize
them using a
[choropleth](https://en.wikipedia.org/wiki/Choropleth_map) but you'd
like to use Haskell. You could try using the
[shapefile](https://hackage.haskell.org/package/shapefile-0.0.0.1)
package but you will have to do a lot of the heavy lifting yourself
(see
[here](https://idontgetoutmuch.wordpress.com/2013/10/23/parking-in-westminster-an-analysis-in-haskell)
for an extended example). But you know folks using R can produce such
things in a few lines of code and you also know from experience that
unless you are very disciplined large R scripts can quickly become
unmaintainable. Can you get the best of both worlds? It seems you can
if you use
[inline-r](https://hackage.haskell.org/package/inline-r). But now you
have a bit of configuration problem: how do you make sure that all the
Haskell packages that you need and all the R packages that you need
are available and how can you make sure that your results are
reproducible? My answer to use [nix](https://nixos.org/nix).

![](diagrams/kingston.png)

Before looking at the *.nix* file I use, let's actually analyse the
data.

https://data.london.gov.uk/dataset/i-trees-canopy-ward-data

~~~~ {include="app/DrawTheWards.hs"}
~~~~

~~~~ {include="fromIssue.nix"}
~~~~

