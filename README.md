# ID codes for the national courts data set

R package to manage ID codes for national corts in the Wallerman et al national 
courts data set. 

## Reading the ID code

The ID codes are composed of five elements. They are structured as follows:

### `1CC2LOC3`

Where:
- `1` refers to the court's position in the judicial hierarchy
- `CC` refers to the country code in which the court is located
- `2` refers to the competence of the court 
- `LOC` refers to the location of the court
- `3` is used to distinguish between non-unique ID codes. A value of `0` can be
      assigned by the *getNatID* function, and  indicates that the tribunal is 
      not listed in the data set. If only one court is found of the given 
      specification, this field is left blank.

For example, `1BE1BRU3` refers to a lower court (1) in Belgium (BE) that is 
general (1) and located in Brussels (BRU), and that is the third (3) court
observed with these specifications. In this case, this ID code refers to the 
*Rechtbank van eerste aanleg te Brussel*; the dutch speaking Court of First 
Instance of Brussels. 

## Functions

### generateID
Adds a leading column of ID codes to the national courts data set.

```R
# Add ID codes to an R data frame containing the national courts data:
data <- generateID(data)
```

### getNatID
Matches names of courts with their appropriate ID codes. 

```R
# Fetch court ID for the German Bundesverfassungsgericht.
# Country does not need to be defined as long as the name of the 
# tribunal is unique. 

getNatID("Bundesverfassungsgericht",
            data = data,
            country = "Germany")

# Where "data" is the national courts data set. 
```

## Interpreting the ID code

The substantial parts of the ID code can be interpreted as follows: 

### Judicial hierarchy:

- 0: Not in hierarchy
- 1: Lower level court
- 2: Mid-level court
- 3: Higher court

### Competence

- 1: General
- 2: Administrative
- 3: Constitutional
- 4: Specialized 

