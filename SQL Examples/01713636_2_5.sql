select a.enterprisenumber
from 
(
	(
	select b.enterprisenumber, cm.type, count(*)
	from branch b
	inner join car c 
		on c.enterprisenumber=b.enterprisenumber
	inner join carmodel cm
		on c.brand=cm.brand and c.model=cm.model 
	group by b.enterprisenumber, cm.type
	order by b.enterprisenumber
		) a 
inner join 	
	(
	select b.enterprisenumber, count(*) as total
	from branch b
	inner join car c 
		on c.enterprisenumber=b.enterprisenumber
	inner join carmodel cm
		on c.brand=cm.brand and c.model=cm.model 
	group by b.enterprisenumber
		) b
	on a.enterprisenumber=b.enterprisenumber
)
where (a.count::float/b.total::float)>=0.3
