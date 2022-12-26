data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Ord, Read)

data CountryCode = CountryCode Integer deriving (Eq, Ord)
data PhoneNo =  PhoneNo Integer deriving (Eq, Ord)

instance Show CountryCode where
    show (CountryCode x) = '+':(show x)

instance Show PhoneNo where
    show (PhoneNo x) = show x

toCountryCode :: Integer -> CountryCode
toCountryCode x
    | x >= 0    = CountryCode x
    | otherwise = error "Negative country code"

toPhoneNo :: Integer -> PhoneNo
toPhoneNo x
    | x >= 0    = PhoneNo x
    | otherwise = error "Negative phone number"

data Phone = Phone  { phoneType :: PhoneType
                    , countryCode :: CountryCode
                    , phoneNo :: PhoneNo
                    } deriving (Eq, Ord)

instance Show Phone where
    show (Phone pt cc pn) = (show cc) ++ " " ++ (show pn) ++ " (" ++ (show pt) ++ ")"

-- makePhone :: PhoneType -> CountryCode  -> PhoneNo -> Phone
-- makePhone t code no
--     | code < 0 = error "Negative country code"
--     | no < 0   = error "Negative phone number"
--     | otherwise = Phone {phoneType=t, countryCode=code, phoneNo=no}
