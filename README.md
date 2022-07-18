# ID codes for the national courts data set

R package to manage ID codes for national corts in the Wallerman et al national 
courts data set. 

## Reading the ID code

The ID codes are composed of five elements. They are structured as follows:

> 1CC2LOC3

Where:
- 1 refers to the court's position in the judicial hierarchy
- CC refers to the country code in which the court is located
- 2 refers to the competence of the court 
- LOC refers to the location of the court
- 3 is used to distinguish between non-unique ID codes.

For example, **1BE1BRU3** refers to a lower court (1) in Belgium (BE) that is 
general (1) and located in Brussels (BRU), and that is the third (3) court
observed with these specifications. In this case, this ID code refers to the 
*Rechtbank van eerste aanleg te Brussel*; the dutch speaking Court of First Instance of Brussels. 

## Functions

As of now, there is only one function in the package. Later, a function to match
court names with ID codes is planned.

### generateID
Adds a leading column of ID codes to the national courts data set.
