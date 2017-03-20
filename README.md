# Imprecision of Measurement

Imprecision of measurement is an unavoidable aspect of laboratory science. If not managed appropriately, results may lack sufficient accuracy to obtain desired results.  In some settings, failure to manage imprecision of measurement may obscure the ability to determine if some measurements are above or below necessary thresholds. 

Imprecision is traditionally managed with _significant figures_, where a significant figure is a digit in a reported value that is believed to have the attribute of precision.  Discussion of significant figures, however, has the potential to distract researchers from the important concepts of measurement imprecision, causing them instead to focus on the number.  This is especially problematic when dealing with values that result from any number of subsequent calculations involving measurements. 

The `imprecise` package adopts a philosophy that precision of measurement is best understood by understanding the limit of precision on the measuring instrument.  After understanding--and documenting--the precision of each instrument, the determination of the appropriate significant figures is a simple exercise. Likewise, when the significant figures of a value are known, the limit of precision may be determined.

## Features of `imprecise`

The key offering of `imprecise` is a new subclass of numeric vectors.  The `measured` class may be declared with the precision of the instrument, if known, added as an attribute.  Alternatively, precision may be declared as the number of significant figures a value is known to  have. When a `measured` value is involved in a calculation, the new methods of arithmetic operators are called to determine the correct precision of the result. Thus, the `measured` clsas  always carries with it the information required to print the value to  the appropriate number of significant figures.  Additionally, `imprecise` includes a `print` method for `measured` that only prints the value to the appropriate precision and/or significant figures.

To assist with printing of results, `label` and `units` attributes may be used (similar to the `Hmisc::label` functions).  

Users are encouraged to read the vignette (`vignette("imprecise")`) for a full discussion of imprecision of measurement in laboratory settings.
