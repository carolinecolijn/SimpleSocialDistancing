### About the Model 
In this very simple model of social distancing,
 a fixed portion of the population is willing 
or able to follow distancing, and for those 
who are, all contacts are reduced by a 
fraction f. This simple model agrees 
well with more complex models that 
consider different kinds of contacts 
(home, work, school, community) 
with opposing effects under 
different kinds of social 
distancing measures. For example, 
with schools closed, household contact 
rates may have a slight rise; some community 
contacts remain even if people work remotely.
 This model assumes homogeneous mixing in the 
population, which despite the complexity of 
human communities, has been shown to do a 
surprisingly good job of reflecting the 
dynamics of respiratory viruses. This model has the advantage that it does not require detailed information about age and contact rates and the simulation and code are simple. 

LIMITATIONS: This model does not have age or contact structure and assumes that the population mixes evenly. We have made guesses about the fraction of contacts that different kinds of activities might represent. These are loosely based on the Imperial College London Report 13, at https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf. In that work the authors used a sophisticated model including households, workplaces, schools and communities. However it is not known what fraction of the COVID-19 transmission opportunities arise through different activities, nor how different age groups contribute to transmission. This model does not explicitly include individuals who may be infectious but never show symptoms. Parameter values are always uncertain (within reason) in any model. 

### More info 

A more in depth description of the model is available [here](http://htmlpreview.github.io/?https://github.com/carolinecolijn/SimpleSocialDistancing/blob/master/SimpleSocialDistanceModel.html) and the code is available on [github](https://github.com/carolinecolijn/SimpleSocialDistancing).
