select c2.license_plate, count(r.license_plate)
from carmodel c1
inner join car c2
on c1.brand=c2.brand and c1.model=c2.model
left join registration r 
on c2.license_plate=r.license_plate
where c1.type='minibus'
group by c2.license_plate having count(*)<=3