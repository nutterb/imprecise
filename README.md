# Imprecision of Measurement

Imprecision of measurement is an unavoidable aspect of laboratory science. If not managed appropriately, results may lack sufficient accuracy to obtain desired results.  In some settings, failure to manage imprecision of measurement may obscure the ability to determine if some measurements are above or below necessary thresholds. 

Imprecision is traditionally managed with _significant figures_, where a significant figure is a digit in a reported value that is believed to have the attribute of precision.  Discussion of significant figures, however, has the potential to distract researchers from the important concepts of measurement imprecision, causing them instead to focus on the number.  This is especially problematic when dealing with values that result from any number of subsequent calculations involving measurements. 

The `imprecise` package adopts a philosophy that precision of measurement is best understood by understanding the limit of precision on the measuring instrument.  After understanding--and documenting--the precision of each instrument, the determination of the appropriate significant figures is a fairly simple exercise.

## Features of `imprecise`

The key offerings of `imprecise` are two new subclasses of numeric vectors.  These are `measured` and `calculated`.  A value may be declared as `measured` with the precision of the instrument added as an attribute.  When a `measured` value is involved in a calculation, the result adopts the `calculated` subclass, which determines for itself the appropriate number of significant figures to use in reporting.  Additionally, `imprecise` includes `print` methods for `measured` and `calculated` that only print the value to the appropriate precision and/or significant figures.

To assist with printing of results, `label` and `units` attributes may be used (similar to the `Hmisc::label` functions).  

Readers are encouraged to read the vignette (`vignette("imprecise")`) for a full discussion of imprecision of measurement in laboratory settings.

## The `measured` class

## The `calculated` class
