SELECT license_plate
FROM car 
WHERE license_plate NOT IN 
	(SELECT DISTINCT license_plate
	FROM registration
	WHERE TO_CHAR(period_begin, 'ID') = '6'
	   OR TO_CHAR(period_begin, 'ID') = '7')
